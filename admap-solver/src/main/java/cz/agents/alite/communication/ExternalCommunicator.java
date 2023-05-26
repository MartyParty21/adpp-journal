package cz.agents.alite.communication;

import cz.agents.alite.communication.content.Content;

/**
 * Communicator set to delegate all the work regarding communication channel and message passing to external listener
 */
public class ExternalCommunicator extends InboxBasedCommunicator {
    /**
     * A listener taking care of sending messages
     */
    private final MessageHandler senderHandler;

    /**
     * Id of the agent the communicator is taking care of
     */
    private final int id;

    /**
     * Number of messages sent
     */
    private int messageCount;

    public ExternalCommunicator(MessageHandler senderHandler, int id) {
        super(String.valueOf(id));
        this.senderHandler = senderHandler;
        this.id = id;
        this.messageCount = 0;
    }

    @Override
    public Message createMessage(Content content) {
        return new Message(String.valueOf(id), content, messageCount++);
    }

    @Override
    public Message createReply(Message message, Content content) {
        return new Message(String.valueOf(id), content, message.getId());
    }

    @Override
    public void addMessageHandler(MessageHandler messageHandler) {
        // Not used for external communicator
    }

    @Override
    public void removeMessageHandler(MessageHandler messageHandler) {
        // Not used for external communicator
    }

    @Override
    public void receiveMessage(Message message) {
        // Not used for external communicator
    }

    @Override
    public void sendMessage(Message message) {
        senderHandler.notify(message);
    }

    @Override
    public String getAddress() {
        return null;
    }

    /**
     * Creates a message with content designated for a Picocluster platform message
     * @param sender serialized UUID of sender node
     * @param content serialized content of message to be sent
     * @param id id of the message to be sent
     * @return new Message containing passed ids and content
     */
    public static Message createMessageFromPicoMessage(String sender, Content content, long id) {
        return new Message(sender, content, id);
    }

    @Override
    public int getInboxSize() {
        return 0;
    }
}
